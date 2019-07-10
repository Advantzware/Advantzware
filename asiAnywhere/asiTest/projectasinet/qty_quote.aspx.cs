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

public partial class qty_quote: System.Web.UI.Page
{
    public static string vcopylog = "" ;
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
        Session["miscprpquote_grid1_index"] = null;
        
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
            if (Session["qty_quote_grid1_index"] != null)
                GridView1.SelectedIndex = Convert.ToInt32(Session["qty_quote_grid1_index"]);
            else
                GridView1.SelectedIndex = 0;
            vcopylog = "";
        }
        /*ImageButton viewquote = (ImageButton)Master.FindControl("ImageButton2");
            viewquote.ImageUrl = "~/Images/quantities 1.jpg";*/
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
        Session["qty_quote_grid1_index"] = GridView1.SelectedIndex;
    }
    
    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        
    }
    
    protected void Formview2_onbatabound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            AddNewButton.Visible = false;
            TextBox desc = (TextBox)FormView2.FindControl("qtqty_qtyTextBox");
            desc.Focus();
        }
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox desc = (TextBox)FormView2.FindControl("qtqty_qtyTextBox");
            Label use = (Label)FormView2.FindControl("qtqty_userTextBox");
            AddNewButton.Visible = false;
            use.Text = UserLogin.UserName ;
            desc.Focus();
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            if(GridView1.Rows.Count > 0)
            AddNewButton.Visible = false;
            else
                AddNewButton.Visible = true;
            try
            {
                Label qty = (Label)FormView2.FindControl("qtqty_qtyLabel");
                Session["MiscprpQuote_main_qty"] = qty.Text.Trim();
            }
            catch { }
        }
    }

    protected void CopyButton_Click(object sender, EventArgs e)
    {
        vcopylog = "Yes";
        
    }
    protected void CancilButton_Click(object sender, EventArgs e)
    {
        vcopylog = "";
    }
    protected void Formview2_update_button_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox qty = (TextBox)FormView2.FindControl("qtqty_qtyTextBox");
        TextBox price = (TextBox)FormView2.FindControl("qtqty_priceTextBox");
        TextBox uom = (TextBox)FormView2.FindControl("qtqty_uomTextBox");
        TextBox profit = (TextBox)FormView2.FindControl("qtqty_profitTextBox");
        TextBox rel = (TextBox)FormView2.FindControl("qtqty_relsTextBox");

        TextBox matcost = (TextBox)FormView2.FindControl("qtqty_matcostTextBox");
        TextBox labcost = (TextBox)FormView2.FindControl("qtqty_labcostTextBox");
        TextBox focost = (TextBox)FormView2.FindControl("qtqty_focostTextBox");
        TextBox vocost = (TextBox)FormView2.FindControl("qtqty_vocostTextBox");
        TextBox date = (TextBox)FormView2.FindControl("qtqty_dateTextBox");
        Label col = (Label)FormView2.FindControl("qtqty_userTextBox");
        QuoteDetail quo = new QuoteDetail();
        bool check = quo.SelectQuoteQtyValidate(UserLogin.UserName, "UpdateValdate", Convert.ToInt32(Session["quote_no"]), 0, 0, uom.Text.Trim(), 0, 0, 0, 0, 0, 0, 0, "", 0, "", "");
        
        string value = Convert.ToString(check);
        if (value == "True")
        {
            if (vcopylog == "Yes")
            {
                ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource_item.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmPrice"].DefaultValue = price.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmUom"].DefaultValue = uom.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmProfit"].DefaultValue = profit.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmRels"].DefaultValue = rel.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmMatCost"].DefaultValue = matcost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmLabCost"].DefaultValue = labcost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmFoCost"].DefaultValue = focost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmVoCost"].DefaultValue = vocost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmDate"].DefaultValue = date.Text.Trim();
                vcopylog = "";
                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='qty_quote.aspx';</script>");
            }
            else
            {

                ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource_item.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmPrice"].DefaultValue = price.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmUom"].DefaultValue = uom.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmProfit"].DefaultValue = profit.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmRels"].DefaultValue = rel.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmMatCost"].DefaultValue = matcost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmLabCost"].DefaultValue = labcost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmFoCost"].DefaultValue = focost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmVoCost"].DefaultValue = vocost.Text.Trim();
                ObjectDataSource_item.SelectParameters["prmDate"].DefaultValue = date.Text.Trim();
                vcopylog = "";
                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='qty_quote.aspx';</script>");
            }
        }

    }

    protected void Formview2_insert_button_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox qty = (TextBox)FormView2.FindControl("qtqty_qtyTextBox");
        TextBox price = (TextBox)FormView2.FindControl("qtqty_priceTextBox");
        TextBox uom = (TextBox)FormView2.FindControl("qtqty_uomTextBox");
        TextBox profit = (TextBox)FormView2.FindControl("qtqty_profitTextBox");
        TextBox rel = (TextBox)FormView2.FindControl("qtqty_relsTextBox");

        TextBox matcost = (TextBox)FormView2.FindControl("qtqty_matcostTextBox");
        TextBox labcost = (TextBox)FormView2.FindControl("qtqty_labcostTextBox");
        TextBox focost = (TextBox)FormView2.FindControl("qtqty_focostTextBox");
        TextBox vocost = (TextBox)FormView2.FindControl("qtqty_vocostTextBox");
        TextBox date = (TextBox)FormView2.FindControl("qtqty_dateTextBox");
        Label col = (Label)FormView2.FindControl("qtqty_userTextBox");

        QuoteDetail quo = new QuoteDetail();

        bool check = quo.SelectQuoteQtyValidate(UserLogin.UserName, "AddValdate", Convert.ToInt32(Session["quote_no"]),0,0, uom.Text.Trim() , 0, 0, 0, 0,0,0,0,"",0,"","");
              
        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Add";
            ObjectDataSource_item.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmPrice"].DefaultValue = price.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmUom"].DefaultValue = uom.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmProfit"].DefaultValue = profit.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmRels"].DefaultValue = rel.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmMatCost"].DefaultValue = matcost.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmLabCost"].DefaultValue = labcost.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmFoCost"].DefaultValue = focost.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmVoCost"].DefaultValue = vocost.Text.Trim();
            ObjectDataSource_item.SelectParameters["prmDate"].DefaultValue = date.Text.Trim();
            
            FormView2.ChangeMode(FormViewMode.ReadOnly);
            Response.Write("<script>window.location.href='qty_quote.aspx';</script>");
        }
    }

    protected void Formview2_deletebutton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["qty_quote_grid1_index"] = null;
        Response.Write("<script>window.location.href='qty_quote.aspx';</script>");
    }
    protected void AddNewButton_Click(object sender1, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
    }

    protected void RepriceButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        RadioButtonList radiolist = (RadioButtonList)FormView2.FindControl("RadioButtonList1");
        string selectval = radiolist.SelectedValue;

        ObjectDataSource_item.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_item.SelectParameters["prmAction"].DefaultValue = "RePrice";
        ObjectDataSource_item.SelectParameters["prmRePrice"].DefaultValue = selectval;

       
       Response.Write("<script>window.location.href='qty_quote.aspx';</script>");
    }
}

