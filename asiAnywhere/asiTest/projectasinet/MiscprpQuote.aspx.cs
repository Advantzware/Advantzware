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

public partial class MiscprpQuote: System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {        
        try
        {
            if (Session["list_rfq_rfq_nos"] != null)
            {
                LinkButton b2quote = (LinkButton)Master.FindControl("list_quote");
                b2quote.Visible = false;
                HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("listquoteh") ;
                rfqlist.Attributes.Add("style", "display:none");
            }
        }
        catch
        {
        }
    }

    protected void Page_Load(object sender, EventArgs e)
    {
              
        if (Session["list_rfq_rfq_nos"] == null)
        {
            try
            {
                LinkButton bquote = (LinkButton)Master.FindControl("ImageButton1");
                bquote.Visible = false;
                HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("rfqlist") ;
                rfqlist.Attributes.Add("style", "display:none");
                Hyperlink1.Visible = false;
            }
            catch { }
        }

           
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
            {                
                                             
                string vUserId = UserLogin.UserName;
                string vPage = "ViewQuote.aspx";
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
            //Label name = (Label)Master.FindControl("lbl_page");
            //name.Text = "View Job";
        if (!Page.IsPostBack)
        {
            if (Session["miscprpquote_grid1_index"] != null)
                GridView1.SelectedIndex = Convert.ToInt32(Session["miscprpquote_grid1_index"]);
            else
                GridView1.SelectedIndex = 0;
        }
        /*ImageButton viewquote = (ImageButton)Master.FindControl("matlinfo");
        viewquote.ImageUrl = "~/Images/prep_misc chg 1.jpg";*/
        }

   
    

    protected void PrintButtonClick(object sender, EventArgs e)
    {
        if (!Request.Browser.Browser.Contains("Safari"))
            Response.Write("<script>window.open('PrintQuote.aspx'); target='_blank'</script>");
        else
            Response.Redirect("BrowseQuote.aspx"); 
    }

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["miscprpquote_grid1_index"] = GridView1.SelectedIndex;
    }
    
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        
    }
    
    protected void Formview2_onbatabound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            AddNewButton.Visible = false;
            TextBox desc = (TextBox)FormView2.FindControl("qtqty_snumTextBox");
            TextBox simon = (TextBox)FormView2.FindControl("qtqty_simonTextBox");
            TextBox bill = (TextBox)FormView2.FindControl("qtqty_billTextBox");
            DropDownList drop = (DropDownList)FormView2.FindControl("DropDownList1");
            DropDownList billdrop = (DropDownList)FormView2.FindControl("DropDownList2");

            if (bill.Text == "L")
                billdrop.SelectedIndex = 0;
            else if (bill.Text == "M")
                billdrop.SelectedIndex = 1;
            else if (bill.Text == "N")
                billdrop.SelectedIndex = 2;
            else if (bill.Text == "T")
                billdrop.SelectedIndex = 3;
            else if (bill.Text == "W")
                billdrop.SelectedIndex = 4;
            
            if (simon.Text == "S")
                drop.SelectedIndex = 0;
            else if (simon.Text == "I")
                drop.SelectedIndex = 1;
            else if (simon.Text == "M")
                drop.SelectedIndex = 2;
            else  if (simon.Text == "N")
                drop.SelectedIndex = 4;
            else
                drop.SelectedIndex = 3;
            desc.Focus();
        }
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox desc = (TextBox)FormView2.FindControl("qtqty_snumTextBox");
            
            AddNewButton.Visible = false;
            desc.Focus();
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            
            if(GridView1.Rows.Count > 0)
            AddNewButton.Visible = false;
            else
                AddNewButton.Visible = true;
        }
    }

    protected void Formview2_update_button_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox S = (TextBox)FormView2.FindControl("qtqty_snumTextBox");
        TextBox B = (TextBox)FormView2.FindControl("qtqty_bnumTextBox");
        TextBox charge = (TextBox)FormView2.FindControl("qtqty_chargeTextBox");
        TextBox prepqty = (TextBox)FormView2.FindControl("qtqty_prepqtyTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("qtqty_costTextBox");

        TextBox makup = (TextBox)FormView2.FindControl("qtqty_mkupTextBox");
        TextBox amtz = (TextBox)FormView2.FindControl("qtqty_amtzTextBox");
        TextBox amount = (TextBox)FormView2.FindControl("qtqty_amtTextBox");
        TextBox matf = (TextBox)FormView2.FindControl("qtqty_matfTextBox");
        TextBox matm = (TextBox)FormView2.FindControl("qtqty_matmTextBox");
        TextBox labf = (TextBox)FormView2.FindControl("qtqty_labfTextBox");
        TextBox labm = (TextBox)FormView2.FindControl("qtqty_labmTextBox");

        DropDownList bill = (DropDownList)FormView2.FindControl("DropDownList2");
        DropDownList simon = (DropDownList)FormView2.FindControl("DropDownList1");
       
        

            ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource_item.SelectParameters["prmSnum"].DefaultValue = S.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmBnum"].DefaultValue = B.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmBill"].DefaultValue = bill.SelectedValue;
            ObjectDataSource_item.SelectParameters["prmCharge"].DefaultValue = charge.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmPrepQty"].DefaultValue = prepqty.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmMkup"].DefaultValue = makup.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmAmtz"].DefaultValue = amtz.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmAmt"].DefaultValue = amount.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmMatF"].DefaultValue = matf.Text.Trim();

            ObjectDataSource_item.SelectParameters["prmMatM"].DefaultValue = matm.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmLabF"].DefaultValue = labf.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmLabM"].DefaultValue = labm.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmSimon"].DefaultValue = simon.SelectedValue;
           
            FormView2.ChangeMode(FormViewMode.ReadOnly);
            Response.Write("<script>window.location.href='MiscprpQuote.aspx';</script>");
        

    }

    protected void Formview2_insert_button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox S = (TextBox)FormView2.FindControl("qtqty_snumTextBox");
        TextBox B = (TextBox)FormView2.FindControl("qtqty_bnumTextBox");
        TextBox charge = (TextBox)FormView2.FindControl("qtqty_chargeTextBox");
        TextBox prepqty = (TextBox)FormView2.FindControl("qtqty_prepqtyTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("qtqty_costTextBox");

        TextBox makup = (TextBox)FormView2.FindControl("qtqty_mkupTextBox");
        TextBox amtz = (TextBox)FormView2.FindControl("qtqty_amtzTextBox");
        TextBox amount = (TextBox)FormView2.FindControl("qtqty_amtTextBox");
        TextBox matf = (TextBox)FormView2.FindControl("qtqty_matfTextBox");
        TextBox matm = (TextBox)FormView2.FindControl("qtqty_matmTextBox");
        TextBox labf = (TextBox)FormView2.FindControl("qtqty_labfTextBox");
        TextBox labm = (TextBox)FormView2.FindControl("qtqty_labmTextBox");

        DropDownList bill = (DropDownList)FormView2.FindControl("DropDownList2");
        DropDownList simon = (DropDownList)FormView2.FindControl("DropDownList1");



        ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource_item.SelectParameters["prmSnum"].DefaultValue = S.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmBnum"].DefaultValue = B.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmBill"].DefaultValue = bill.SelectedValue;
        ObjectDataSource_item.SelectParameters["prmCharge"].DefaultValue = charge.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmPrepQty"].DefaultValue = prepqty.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmMkup"].DefaultValue = makup.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmAmtz"].DefaultValue = amtz.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmAmt"].DefaultValue = amount.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmMatF"].DefaultValue = matf.Text.Trim();

        ObjectDataSource_item.SelectParameters["prmMatM"].DefaultValue = matm.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmLabF"].DefaultValue = labf.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmLabM"].DefaultValue = labm.Text.Trim();
        ObjectDataSource_item.SelectParameters["prmSimon"].DefaultValue = simon.SelectedValue;

        FormView2.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='MiscprpQuote.aspx';</script>");
    }

    protected void Formview2_deletebutton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["miscprpquote_grid1_index"] = null;
        Response.Write("<script>window.location.href='MiscprpQuote.aspx';</script>");
    }
    protected void AddNewButton_Click(object sender1, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
    }
}

