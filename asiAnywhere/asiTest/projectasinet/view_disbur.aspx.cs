
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class view_disbur : System.Web.UI.Page
{
    //string oldinv = "";
    //string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "disbur_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {

                if (Convert.ToString(Session["disbur_list_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["disbur_list_add_button"] = null;
                }
                

            }
            

        } //  ! Page.IsPostBack

        if (Session["view_disbur_reckey_index"] != null)
        {
            try
            {
                GridView1.SelectedIndex = Convert.ToInt32(Session["view_disbur_reckey_index"]);
                Session["view_disbur_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_list_notes_rec_key"] = Session["view_disbur_reckey"];
            }
            catch { }
        }
        else
        {
            try
            {
                GridView1.SelectedIndex = 0;
                Session["view_disbur_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                Session["top_list_notes_rec_key"] = Session["view_disbur_reckey"];
            }
            catch { }
        }


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
           TextBox vendorname = (TextBox)FormView1.FindControl("vendnameLabel");
           TextBox checkno = (TextBox)FormView1.FindControl("ChecknoTextBox");
           TextBox date = (TextBox)FormView1.FindControl("chkdateTextBox");
           TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
           TextBox bank = (TextBox)FormView1.FindControl("bankcodeTextBox");
           TextBox bankname = (TextBox)FormView1.FindControl("banknameTextBox");
           TextBox payee = (TextBox)FormView1.FindControl("payeeTextBox");
           TextBox curr = (TextBox)FormView1.FindControl("currcodTextBox");
           TextBox exg = (TextBox)FormView1.FindControl("exchTextBox");
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

           vendor.Focus();
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
           try
           {
               UserClass UserLogin = (UserClass)Session["User"];
               voucherpay con = new voucherpay();
               DataSet ds = new DataSet();
               ds = con.SelectDisbursements("AddNewRecord", "", UserLogin.UserName, "", "", 0, "", "", 0, "", "", "", 0, "");

               //vendor.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
               //vendorname.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
               bank.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
               bankname.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
               checkno.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
               date.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
               amt.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
               payee.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
               curr.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
               exg.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
               reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
               
              
           }
           catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");           
           vendor.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           
           try
           { 
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["disbur_list_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyLabel");
       UserClass UserLogin = (UserClass)Session["User"];

       try
       {
           ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
           ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

           FormView1.ChangeMode(FormViewMode.ReadOnly);

           Response.Write("<script>window.location.href='view_disbur.aspx'</script>");
       }
       catch { }

   }

   protected void Formview1_InsertCancelButtonClick(object sender, EventArgs e)
   {
       try
       {
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
           UserClass UserLogin = (UserClass)Session["User"];
           voucherpay con = new voucherpay();
           DataSet ds = new DataSet();
           ds = con.SelectDisbursements("DataDelete", "", UserLogin.UserName, "", "", 0, "", "", 0, "", "", "", 0, reckey.Text.Trim());

       }
       catch { }

   }

   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");       
       TextBox checkno = (TextBox)FormView1.FindControl("ChecknoTextBox");
       TextBox date = (TextBox)FormView1.FindControl("chkdateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
       TextBox bank = (TextBox)FormView1.FindControl("bankcodeTextBox");       
       TextBox payee = (TextBox)FormView1.FindControl("payeeTextBox");       
       TextBox exg = (TextBox)FormView1.FindControl("exchTextBox");       

       if (checkno.Text == "")
           checkno.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";


       UserClass UserLogin = (UserClass)Session["User"];
       ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
       ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();
       ObjectDataSource1.SelectParameters["prmcheckno"].DefaultValue = checkno.Text.Trim();
       ObjectDataSource1.SelectParameters["prmcheckdate"].DefaultValue = date.Text.Trim();
       ObjectDataSource1.SelectParameters["prmpayee"].DefaultValue = payee.Text.Trim();
       ObjectDataSource1.SelectParameters["prmcheckamt"].DefaultValue = amt.Text.Trim();
       ObjectDataSource1.SelectParameters["prmbnkcod"].DefaultValue = bank.Text.Trim();
       ObjectDataSource1.SelectParameters["prmexrat"].DefaultValue = exg.Text.Trim();
       

       FormView1.ChangeMode(FormViewMode.ReadOnly);
       

   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
       TextBox checkno = (TextBox)FormView1.FindControl("ChecknoTextBox");
       TextBox date = (TextBox)FormView1.FindControl("chkdateTextBox");
       TextBox amt = (TextBox)FormView1.FindControl("amtTextBox");
       TextBox bank = (TextBox)FormView1.FindControl("bankcodeTextBox");
       TextBox payee = (TextBox)FormView1.FindControl("payeeTextBox");
       TextBox exg = (TextBox)FormView1.FindControl("exchTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

       if (checkno.Text == "")
           checkno.Text = "0";
       if (amt.Text == "")
           amt.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       
       voucherpay val = new voucherpay();

       bool check = val.ValidateDisbursements("ValidateAdd", "", UserLogin.UserName, Convert.ToString(vendor.Text.Trim()), "", Convert.ToInt32(checkno.Text.Trim()), Convert.ToString(date.Text.Trim()), Convert.ToString(payee.Text.Trim()), Convert.ToDecimal(amt.Text.Trim()), Convert.ToString(bank.Text.Trim()), "", "", Convert.ToDecimal(exg.Text.Trim()), Convert.ToString(reckey.Text.Trim()));

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["disbur_list_reckey_rec"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcheckno"].DefaultValue = checkno.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcheckdate"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmpayee"].DefaultValue = payee.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcheckamt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnkcod"].DefaultValue = bank.Text.Trim();
                ObjectDataSource1.SelectParameters["prmexrat"].DefaultValue = exg.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                FormView2.ChangeMode(FormViewMode.Insert);
            }
            
   }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_disbur.aspx");
    }
    
    protected void lnk_listcash(object sender, EventArgs e)
    {
        Response.Redirect("disbur_list.aspx");
    }
    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_disbur_reckey_index"] = GridView1.SelectedIndex;
        Session["view_disbur_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["top_list_notes_rec_key"] = Session["view_disbur_reckey"];
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox line = (TextBox)FormView2.FindControl("lineTextBox");
        TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("qtyTextBox");
        TextBox unt = (TextBox)FormView2.FindControl("untprTextBox");
        TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");              

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (qty.Text == "")
            qty.Text = "0";
        if (unt.Text == "")
            unt.Text = "0";
        if (amt.Text == "")
            amt.Text = "0";
        UserClass UserLogin = (UserClass)Session["User"];
        
        voucherpay val = new voucherpay();

        bool check = val.ValidateViewDisbursements("UpdateValidate", "", UserLogin.UserName, Convert.ToInt32(line.Text.Trim()), Convert.ToString(dscr.Text.Trim()), Convert.ToString(act.Text.Trim()), Convert.ToDecimal(qty.Text.Trim()), Convert.ToDecimal(unt.Text.Trim()), Convert.ToDecimal(amt.Text.Trim()), Convert.ToString(Session["disbur_list_reckey_rec"]), "");

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmvline"].DefaultValue = line.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactnum"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmqty"].DefaultValue = qty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmuntprice"].DefaultValue = unt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmreckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_disbur.aspx'</script>");
            }
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox line = (TextBox)FormView2.FindControl("lineTextBox");
        TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("qtyTextBox");
        TextBox unt = (TextBox)FormView2.FindControl("untprTextBox");
        TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (qty.Text == "")
            qty.Text = "0";
        if (unt.Text == "")
            unt.Text = "0";
        if (amt.Text == "")
            amt.Text = "0";
        
        UserClass UserLogin = (UserClass)Session["User"];

        voucherpay val = new voucherpay();

        bool check = val.ValidateViewDisbursements("AddnewRecValidate", "", UserLogin.UserName, Convert.ToInt32(line.Text.Trim()), Convert.ToString(dscr.Text.Trim()), Convert.ToString(act.Text.Trim()), Convert.ToDecimal(qty.Text.Trim()), Convert.ToDecimal(unt.Text.Trim()), Convert.ToDecimal(amt.Text.Trim()), Convert.ToString(Session["disbur_list_reckey_rec"]), "");

        string value = Convert.ToString(check);
        if (value == "True")
        {
            Session["view_disbur_reckey"] = reckey.Text.Trim();
            ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";
            //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
            ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource3.SelectParameters["prmvline"].DefaultValue = line.Text.Trim(); ;
            ObjectDataSource3.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
            ObjectDataSource3.SelectParameters["prmactnum"].DefaultValue = act.Text.Trim();
            ObjectDataSource3.SelectParameters["prmqty"].DefaultValue = qty.Text.Trim();
            ObjectDataSource3.SelectParameters["prmuntprice"].DefaultValue = unt.Text.Trim();
            ObjectDataSource3.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
            ObjectDataSource3.SelectParameters["prmreckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_disbur.aspx'</script>");

            }
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox line = (TextBox)FormView2.FindControl("lineTextBox");
            TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
            TextBox act = (TextBox)FormView2.FindControl("actTextBox");
            TextBox qty = (TextBox)FormView2.FindControl("qtyTextBox");
            TextBox unt = (TextBox)FormView2.FindControl("untprTextBox");
            TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");
            if (line.Text == "")
                line.Text = "0";
            if (qty.Text == "")
                qty.Text = "0";
            if (unt.Text == "")
                unt.Text = "0";
            if (amt.Text == "")
                amt.Text = "0";
            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
            dscr.Focus();

            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                voucherpay val = new voucherpay();

                bool check = val.ValidateViewDisbursements("AddnewRecValidate", "", UserLogin.UserName, Convert.ToInt32(line.Text.Trim()), Convert.ToString(dscr.Text.Trim()), Convert.ToString(act.Text.Trim()), Convert.ToDecimal(qty.Text.Trim()), Convert.ToDecimal(unt.Text.Trim()), Convert.ToDecimal(amt.Text.Trim()), "", Convert.ToString(Session["disbur_list_reckey_rec"]));

                string value = Convert.ToString(check);
                if (value == "True")
                {
                    voucherpay con = new voucherpay();
                    DataSet ds = new DataSet();
                    ds = con.SelectViewDisbursements("AddnewRec", "", UserLogin.UserName, 0, "", "", 0, 0, 0, "", Convert.ToString(Session["disbur_list_reckey_rec"]));

                    line.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
                    dscr.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                    act.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                    qty.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);

                    unt.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
                    amt.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);


                    reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);


                    GridView1.Visible = false;
                }
                else
                {
                    FormView2.ChangeMode(FormViewMode.ReadOnly);
                }
            }
            catch { }
                                  

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
            dscr.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_disbur_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            
            try
            {
                if (FormView2.DataItemCount.ToString() == "0")
                    AddNewFormView2Button.Visible = true;
                else
                    AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        UserClass UserLogin = (UserClass)Session["User"];
        voucherpay con = new voucherpay();
        DataSet ds = new DataSet();

        ds = con.SelectViewDisbursements("Delete", "", UserLogin.UserName, 0, "", "", 0, 0, 0, reckey.Text.Trim(), Convert.ToString(Session["disbur_list_reckey_rec"]));
        
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        //Response.Write("<script>window.location.href='view_manchk.aspx'</script>");
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
               
        voucherpay val = new voucherpay();

        bool check = val.ValidateViewDisbursements("Deletevalidate", "", UserLogin.UserName, 0, "", "", 0, 0, 0, Convert.ToString(Session["view_disbur_reckey"]), Convert.ToString(Session["disbur_list_reckey_rec"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {


            ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Delete";
            ObjectDataSource3.SelectParameters["prmchkno"].DefaultValue = Convert.ToString(Session["disbur_reckey_rec"]);
            Response.Write("<script>window.location.href='view_disbur.aspx'</script>");
        }

    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
    }

    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }    

    

    
    
    

}
