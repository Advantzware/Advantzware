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

public partial class inventory : System.Web.UI.Page
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
            }
        }
        if (Session["User"] != null)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            UserClass.CheckLogin(Page);

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
           // ObjectDataSource1.UpdateParameters["prmUser"].DefaultValue = UserLogin.UserName;

            string vUserId = UserLogin.UserName;
            string vPage = "inventory.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
           
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        //ImageButton inventory = (ImageButton)Master.FindControl("inventory");
        //inventory.ImageUrl = "~/img/inventory1.jpg";
        Label name = (Label)Master.FindControl("lbl_page");
        name.Text = "Inventory";
        //try
        //{
            Button btnbrowsorder = (Button)FormView1.FindControl("Button8");
            Button btnonorder = (Button)FormView1.FindControl("Button1");
            btnbrowsorder.Visible = true;
            btnonorder.Visible = true;
            string estimate = Convert.ToString(Session["orderestimate121"]);
            if (estimate == "&nbsp;")
            {
                btnbrowsorder.Visible = false;
            }
            else
            {
                btnonorder.Visible = false;

            }
        //}
        //catch
        //{
        //    return;
        //}

        if (!Page.IsPostBack)
        {
            CheckBox orderpolicy = (CheckBox)FormView1.FindControl("OrdPolicyCheckBox");
            RadioButton reorder = (RadioButton)FormView1.FindControl("radioreorder");
            RadioButton lotcontrolled = (RadioButton)FormView1.FindControl("radiolotcontrolled");
            if (orderpolicy.Checked == true)
            {
                reorder.Checked = true;
            }
            else
            {
                lotcontrolled.Checked = true;
            }
                       
            CheckBox purchased = (CheckBox)FormView1.FindControl("PurchasedCheckBox");
            RadioButton purchase = (RadioButton)FormView1.FindControl("RD1");
            RadioButton manufacture = (RadioButton)FormView1.FindControl("RD2");
            if (purchased.Checked == true)
            {
                purchase.Checked = true;
            }
            else
            {
                manufacture.Checked = true;
            }
        }
    }

    protected void Button7_Click(object sender, EventArgs e)
    {
        Label inventoryonhand = (Label)FormView1.FindControl("q_onhLabel");
        if (inventoryonhand.Text == "0")
        {
            return;
        }
        else
        {
            if (Session["order"] != null)
            {
                Session["bin"] = Session["order"];
                Response.Redirect("binjobs.aspx");
            }
        }
    }
    protected void Button8_Click(object sender, EventArgs e)
    {
        Label onorder = (Label)FormView1.FindControl("q_onoLabel");
        
        if (onorder.Text == "0")
        {
            return;
        }
        else
        {            
            if (Session["order"] != null)
            {
                Session["show"] = 11;
                Session["jobprod"] = Session["order"];
                Session["brwsjobs_item_num"] = Session["item"];
                Response.Redirect("brwsjobs.aspx");
            }
        }
    }

    protected void Button1_Click(object sender, EventArgs e)
    {
        Label onorder = (Label)FormView1.FindControl("q_onoLabel");

        if (onorder.Text == "0")
        {
            return;
        }
        else
        {
            if (Session["order"] != null)
            {
                Session["BrowsePO"] = Session["order"];
                Session["item_list_item"] = Session["item_list_item"];

                Response.Redirect("browsepo.aspx");
            }
        }
    }

    protected void Button9_Click(object sender, EventArgs e)
    {
        Label allocated = (Label)FormView1.FindControl("q_allocLabel");
        if (allocated.Text == "0")
        {
            return;
        }
        else
        {
            if (Session["item"] != null)
            {
                Session["fgitem1"] = Session["item_list_item"];
                Response.Redirect("order_inquiry.aspx");
            }
        }
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
}
