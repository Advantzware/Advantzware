using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class setpart : System.Web.UI.Page
{

    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["Item_Inquiry_value_check"] != null)
        {
            LinkButton1.Visible = false;
            LinkButton2.Visible = false;
            LinkButton3.Visible = false;
        }
        else
        {
            //ImageButton iteminquiry = (ImageButton)Master.FindControl("listitem");
            //iteminquiry.Visible = false;
            HtmlGenericControl iteminquiry = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            iteminquiry.Attributes.Add("style", "display:none");
            if (Session["view_order_entry_pages"] == null)
            {
                LinkButton2.Visible = false;
                
            }
            if (Session["view_order_entry_pages"] != null)
            {
                LinkButton1.Visible = false;
                
            }

            if (Session["view_order_entry_pages_with_estimate"] != null)
            {
                LinkButton1.Visible = false;
                LinkButton2.Visible = false;
               
            }
            if (Session["view_order_entry_pages_with_estimate"] == null)
            {
                LinkButton3.Visible = false;
                FormView1.Visible = false;
                
            }
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "fgitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblComp.Text = PrmComp;
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        ObjectDataSource_setpart.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        GridView1.DataBind();
        try
        {

            GridView1.SelectedIndex = Convert.ToInt32(Session["fgitem_set_part_rec_index"]);
            if (Session["fgitem_set_part_rec_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["fgitem_set_part_rec"] = ((Label)GridView1.SelectedRow.FindControl("vReckeyLabel")).Text;
            }
        }
                   
        catch { }


        //ImageButton setparts = (ImageButton)Master.FindControl("setparts");
        //setparts.ImageUrl = "~/img/setparts1.jpg";
        if (!Page.IsPostBack)
        {
            try
            {
               
            }
            catch
            {
                
            }

            
        }
    }
    protected void FormView1_ItemUpdated(object sender, FormViewUpdatedEventArgs e)
    {
        //this.FormView1.DataBind();
    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue =UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update" ;
        ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
        Response.Write("<script>window.location.href='setpart.aspx'</script>");
    }
    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        TextBox fgitem = (TextBox)FormView1.FindControl("vFGPartTextBox");

        fgitem setpart = new fgitem();
        
        bool check = setpart.SetPartVal(UserLogin.UserName, "Add", "", Convert.ToInt32("0"), fgitem.Text.Trim());

        
        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
            ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
            ObjectDataSource1.SelectParameters["prmNewFgItem"].DefaultValue = fgitem.Text.Trim();
            Response.Write("<script>window.location.href='setpart.aspx'</script>");
        }

    }

    protected void Delete_Button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView1.FindControl("vReckeyLabel");
        Session["fgitem_set_part_rec"] = reckey.Text.Trim();

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = Convert.ToString(Session["fgitem_set_part_rec"]);
        Response.Write("<script>window.location.href='setpart.aspx'</script>");
         
    }

    protected void formview1_unload(object sender, EventArgs e)
    {
        try
        {
            
            Label reckey = (Label)FormView1.FindControl("vReckeyLabel");
            if (reckey.Text != "")
            {
                Session["fgitem_set_part_rec"] = reckey.Text.Trim();
            }
        }
        catch { }
    }

    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }

    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["fgitem_set_part_rec_index"] = GridView1.SelectedIndex;
        Session["fgitem_set_part_rec"] = ((Label)GridView1.SelectedRow.FindControl("vReckeyLabel")).Text;
        
    }
    protected void GridView1_RowDataBound(object sender, EventArgs e)
    {
    }
}