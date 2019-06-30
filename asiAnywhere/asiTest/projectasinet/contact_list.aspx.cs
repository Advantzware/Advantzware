
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class Ccontact_list : System.Web.UI.Page
{

    private const string ASCENDING = " ASC";
    private const string DESCENDING = " DESC";
    string userlog = "";   

    //private bool bSort = true;

    protected void Page_PreRender(object sender, EventArgs e)
    {
        BuildDataSource();
       
    }
    protected void Page_Load(object sender, System.EventArgs e)
    {
        Session["Calendar_grid_index"] = null;
        Session["Current_date"] = null;
        Session["view_contact_list"] = 1;
        Session["view_comp_supplier"] = null;
        Session["list_notes_date"] = null;
        Session["list_notes_time"] = null;
        Session["list_notes_index"] = null;


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "contact_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            userlog = aUsers;
           
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }



        if (!Page.IsPostBack)
        {
            lblUser.Text = UserLogin.UserName;
            //GridViewSortDirection = SortDirection.Descending;
        }



        lblMessage.Text = "";
        dbGrid_contact.Visible = true;

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }
        Session["Rowuser"] = UserLogin.UserName;
        dbGrid_contact.SelectedIndex = Convert.ToInt32(Session["contact_list_index"]);
        try
        {
            if (Session["contact_list_index"] == null)
            {
                dbGrid_contact.SelectedIndex = 0;
                Session["cust_type_comp"] = Session["cust_type_comp"];
                Session["contact_list_cust_no"] = dbGrid_contact.SelectedRow.Cells[29].Text;
                Session["contact_list_last_name"] = dbGrid_contact.SelectedRow.Cells[26].Text;
                Session["contact_list_first_name"] = dbGrid_contact.SelectedRow.Cells[27].Text;
                Session["contact_list_contact_title"] = dbGrid_contact.SelectedRow.Cells[28].Text;
                Session["contact_list_ship_id"] = dbGrid_contact.SelectedRow.Cells[30].Text;
                Session["contact_list_type"] = dbGrid_contact.SelectedRow.Cells[31].Text;
                Session["contact_list_phone"] = dbGrid_contact.SelectedRow.Cells[32].Text;
                Session["contact_list_extension"] = dbGrid_contact.SelectedRow.Cells[33].Text;
                Session["contact_list_cust_name"] = dbGrid_contact.SelectedRow.Cells[34].Text;


                foreach (GridViewRow gv in dbGrid_contact.Rows)
                {
                    Session["contact_list_sman"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label1")).Text;
                    Session["contact_list_middle_initial"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label2")).Text;
                    Session["contact_list_sirname"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label3")).Text;
                    Session["contact_list_maillist"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label4")).Text;
                    Session["contact_list_contact_loc"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label5")).Text;
                    Session["contact_list_addr1"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label6")).Text;
                    Session["contact_list_addr2"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label7")).Text;
                    Session["contact_list_city"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label8")).Text;
                    Session["contact_list_state"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label9")).Text;

                    Session["contact_list_zip"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label10")).Text;
                    Session["contact_list_country"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label11")).Text;
                    Session["contact_list_county"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label12")).Text;
                    Session["contact_list_territory"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label13")).Text;
                    Session["contact_list_access_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label14")).Text;
                    Session["contact_list_cell_phone"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label15")).Text;

                    Session["contact_list_fax"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label16")).Text;
                    Session["contact_list_email"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label17")).Text;
                    Session["contact_list_website"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label18")).Text;
                    //Response.Write(Session["contact_list_cust_no"]);
                    Session["contact_list_comp_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label19")).Text;
                    Session["contact_list_status_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label20")).Text;
                    Session["contact_list_sic_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label21")).Text;
                    Session["contact_rec_key"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label22")).Text;

                    Session["contact_comp_des"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label23")).Text;
                    Session["contact_status_des"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label24")).Text;
                    Session["contact_sic_des"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label25")).Text;
                }

            }
        }
        catch
        {
            //return;
        }


        Session["Rowuser"] = UserLogin.UserName;
        try
        {
            TextBox ddl_display2 = (TextBox)FormView3.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display2.Text);
            dbGrid_contact.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
           // return;
        }



    }

    private void BuildDataSource()
    {
        string vman = "";
        string vcust = "";
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        browspo userman = new browspo();
        DataSet dsman = new DataSet();
        DataSet dscust = new DataSet();

        dsman = userman.SelectUserManTable("Select", "", UserLogin.UserName, "", "", "","", "","");
        dscust = userman.SelectUserManTable("Customer", "", UserLogin.UserName, "", "", "", "", "", "");
        try
        {
            for (int j = 0; j < dsman.Tables[0].Rows.Count; j++)
            {
                vman = vman + dsman.Tables[0].Rows[j][0].ToString().Trim().ToString() + ",";                
            }
        }
        catch { }
        try
        {
            for (int j = 0; j < dscust.Tables[0].Rows.Count; j++)
            {
                vcust = vcust + dscust.Tables[0].Rows[j][0].ToString().Trim().ToString() + ",";
            }
        }
        catch { }

        
        string[] arrSman = vman.Split(new char[] { ',' });
        string[] arrCust = vcust.Split(new char[] { ',' });

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        if (userlog == "external")
        {
            try
            {
                conn.Open();
                string cmd = "";
                string newcmd = "";
                string newcustcmd = "";
                try
                {
                    for (int i = 0; i < arrSman.Length - 1; i++)
                    {
                        newcmd = newcmd + "'" + arrSman[i] + "'" + ",";
                    }
                    newcmd = newcmd.Remove(newcmd.Length - 1, 1);

                }
                catch { }
                try
                {
                    for (int i = 0; i < arrCust.Length - 1 ; i++)
                    {
                        newcustcmd = newcustcmd + "'" + arrCust[i] + "'" + ",";
                    }
                    newcustcmd = newcustcmd.Remove(newcustcmd.Length - 1, 1);
                }
                catch { }

                if (newcmd == "" || newcmd == null)
                    newcmd = "'a1234b1234c1234'";
                if (newcustcmd == "" || newcustcmd == null)
                    newcustcmd = "'a1234b1234c1234'";
               
       
                cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code' , maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ") ";


                if (txt_lastname.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }

                if (txt_customer.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where ship_id LIKE '" + txt_shipid.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_siccode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where status_code = '" + txt_statuscode.Text + "' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sman LIKE '" + txt_rep.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_rep.Text != "" && txt_customer.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_rep.Text != "" && txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where ship_id LIKE '" + txt_shipid.Text + "%' and sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_rep.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%' and sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_rep.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_siccode.Text + "&' and sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_rep.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where status_code LIKE '" + txt_statuscode.Text + "%' and sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }


                if (txt_lastname.Text != "" && txt_customer.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where ship_id LIKE '" + txt_shipid.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_siccode.Text + "&' and last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where status_code LIKE '" + txt_statuscode.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }

                if (txt_customer.Text != "" && txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and ship_id LIKE '" + txt_shipid.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_customer.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and type like '" + txt_type.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_customer.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_customer.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and ship_id like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }

                if (txt_type.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_type.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and ship_id like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }


                if (txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'  and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_customer.Text != "" && txt_lastname.Text != "" && txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and last_name like '" + txt_lastname.Text + "%' and sman LIKE '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_type.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }

                if (txt_type.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_type.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and type like '" + txt_type.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }

                if (txt_lastname.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "" && txt_shipid.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and ship_id like '" + txt_shipid.Text + "%' and type like '" + txt_type.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }
                if (txt_lastname.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "" && txt_shipid.Text != "" && txt_type.Text != "" && txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and ship_id like '" + txt_shipid.Text + "%' and type like '" + txt_type.Text + "%' and sman like '" + txt_rep.Text + "%' and sman in (" + newcmd + ") and cust_no in (" + newcustcmd + ")";
                }


                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                try
                {
                    da.Fill(ds);

                    DataView dv = ds.Tables[0].DefaultView;
                    if (Session["contact_list_grid_sort_ex"] == null)
                    {
                        dv.Sort = "Customer ASC";
                    }
                    if (Session["contact_list_grid_sort_ex"] != null)
                    {
                        dv.Sort = Convert.ToString(Session["contact_list_grid_sort_ex"]);
                    }

                    dbGrid_contact.DataSource = dv;
                    dbGrid_contact.DataBind();
                }
                catch { }

                conn.Close();


            }
            catch {  }
            finally
            {
                conn.Close();
            }
        }   /*extrnal   */
        if (userlog == "internal")
        {
            try
            {
                conn.Open();
                string cmd = "";

                cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code' , maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact ";


                if (txt_lastname.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name LIKE '" + txt_lastname.Text + "%'";
                }
                if (txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sman LIKE '" + txt_rep.Text + "%'";
                }

                if (txt_customer.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%'";
                }
                if (txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where ship_id LIKE '" + txt_shipid.Text + "%'";
                }
                if (txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%'";
                }
                if (txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_siccode.Text + "%'";
                }
                if (txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where status_code = '" + txt_statuscode.Text + "'";
                }
                if (txt_lastname.Text != "" && txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sman LIKE '" + txt_rep.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%'";
                }
                if (txt_rep.Text != "" && txt_customer.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and sman LIKE '" + txt_rep.Text + "%'";
                }
                if (txt_rep.Text != "" && txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where ship_id LIKE '" + txt_shipid.Text + "%' and sman LIKE '" + txt_rep.Text + "%'";
                }
                if (txt_rep.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%' and sman LIKE '" + txt_rep.Text + "%'";
                }
                if (txt_rep.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_siccode.Text + "&' and sman LIKE '" + txt_rep.Text + "%'";
                }
                if (txt_rep.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where status_code LIKE '" + txt_statuscode.Text + "%' and sman LIKE '" + txt_rep.Text + "%'";
                }


                if (txt_lastname.Text != "" && txt_customer.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where ship_id LIKE '" + txt_shipid.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type LIKE '" + txt_type.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code LIKE '" + txt_siccode.Text + "&' and last_name LIKE '" + txt_lastname.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where status_code LIKE '" + txt_statuscode.Text + "%' and last_name LIKE '" + txt_lastname.Text + "%'";
                }

                if (txt_customer.Text != "" && txt_shipid.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no LIKE '" + txt_customer.Text + "%' and ship_id LIKE '" + txt_shipid.Text + "%'";
                }
                if (txt_customer.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and type like '" + txt_type.Text + "%'";
                }
                if (txt_customer.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%'";
                }
                if (txt_customer.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and ship_id like '" + txt_statuscode.Text + "%'";
                }

                if (txt_type.Text != "" && txt_siccode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and sic_code like '" + txt_siccode.Text + "%'";
                }
                if (txt_type.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name',sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and ship_id like '" + txt_statuscode.Text + "%'";
                }


                if (txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }
                if (txt_customer.Text != "" && txt_lastname.Text != "" && txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and last_name like '" + txt_lastname.Text + "%' and sman LIKE '" + txt_rep.Text + "%' ";
                }
                if (txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }
                if (txt_type.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }

                if (txt_type.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where type like '" + txt_type.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_type.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and type like '" + txt_type.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%'";
                }

                if (txt_lastname.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "" && txt_shipid.Text != "" && txt_type.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and ship_id like '" + txt_shipid.Text + "%' and type like '" + txt_type.Text + "%'";
                }
                if (txt_lastname.Text != "" && txt_customer.Text != "" && txt_siccode.Text != "" && txt_statuscode.Text != "" && txt_shipid.Text != "" && txt_type.Text != "" && txt_rep.Text != "")
                {
                    cmd = "select last_name as 'Last Name', first_name as 'First Name', sman as 'Rep', contact_title as 'Title', cust_no as 'Customer', ship_id as 'ShipId', type as 'Type', phone as 'Phone', extension as 'Extension', cust_name as 'Company' ,status_code as 'Status Code', sic_code as 'Sic Code', maillist as 'Mail List', sman, middle_initial, sirname, maillist, contact_loc, addr1, addr2, city, state, zip, country, county, territory, access_code, cell_phone, fax, email, website, comp_code,status_code, sic_code, comp_des,status_des,sic_des, rec_key  from contact where last_name like '" + txt_lastname.Text + "%' and cust_no like '" + txt_customer.Text + "%' and sic_code like '" + txt_siccode.Text + "%' and status_code like '" + txt_statuscode.Text + "%' and ship_id like '" + txt_shipid.Text + "%' and type like '" + txt_type.Text + "%' and sman like '" + txt_rep.Text + "%' ";
                }


                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                try
                {
                    da.Fill(ds);

                    DataView dv = ds.Tables[0].DefaultView;
                    if (Session["contact_list_grid_sort_ex"] == null)
                    {
                        dv.Sort = "Customer ASC";
                    }
                    if (Session["contact_list_grid_sort_ex"] != null)
                    {
                        dv.Sort = Convert.ToString(Session["contact_list_grid_sort_ex"]);
                    }

                    dbGrid_contact.DataSource = dv;
                    dbGrid_contact.DataBind();
                }
                catch { }

                conn.Close();


            }
            catch { }
            finally
            {
                conn.Close();
            }
        } /*Internam  */
    }

    protected void dbGrid_contact_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
            
           // e.Row.Cells[38].Visible = false;
            e.Row.Cells[39].Visible = false;
            e.Row.Cells[40].Visible = false;
            e.Row.Cells[41].Visible = false;
            e.Row.Cells[42].Visible = false;
            e.Row.Cells[43].Visible = false;
            e.Row.Cells[44].Visible = false;
            e.Row.Cells[45].Visible = false;
            e.Row.Cells[46].Visible = false;
            e.Row.Cells[47].Visible = false;
            e.Row.Cells[48].Visible = false;
            e.Row.Cells[49].Visible = false;
            e.Row.Cells[50].Visible = false;
            e.Row.Cells[51].Visible = false;
            e.Row.Cells[52].Visible = false;
            e.Row.Cells[53].Visible = false;
            e.Row.Cells[54].Visible = false;
            e.Row.Cells[55].Visible = false;
            e.Row.Cells[56].Visible = false;
            e.Row.Cells[57].Visible = false;
            e.Row.Cells[58].Visible = false;
            e.Row.Cells[59].Visible = false;
            e.Row.Cells[60].Visible = false;
            e.Row.Cells[61].Visible = false;
            e.Row.Cells[62].Visible = false;
            e.Row.Cells[63].Visible = false;

        }
        catch {  }

    }

    protected void dbGrid_contact_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        dbGrid_contact.PageIndex = e.NewPageIndex;
      
    }
    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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

    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        ViewState["bNoRecords"] = false;
        txt_lastname.Text = "";
        txt_customer.Text = "";
        //txt_company.Text = "";
        txt_shipid.Text = "";
        txt_siccode.Text = "";
        txt_statuscode.Text = "";
        txt_type.Text = "";
        txt_rep.Text = "";
        //ddlSearchField.SelectedIndex = 0;

        Session["dbGrid_contact_CurrentPageIndex"] = 0;
        Session["dbGrid_contact_SearchSQL"] = null;
        Session["dbGrid_contact_SearchParams"] = null;
        Session["dbGrid_contact_AdvSearch"] = null;
        Session["dbGrid_contact_AdvParam"] = null;
        Session["htPeramcontact"] = null;

        Session["htPeramcontact"] = null;

        Session["contact_list_index"] = null;
        Session["contact_list_cust_no"] = null;
        Session["contact_list_last_name"] = null;
        Session["contact_list_first_name"] = null;
        Session["contact_list_contact_title"] = null;
        Session["contact_list_ship_id"] = null;
        Session["contact_list_type"] = null;
        Session["contact_list_phone"] = null;
        Session["contact_list_extension"] = null;
        Session["contact_list_cust_name"] = null;
        Session["contact_list_sman"] = null;
        Session["contact_list_middle_initial"] = null;
        Session["contact_list_sirname"] = null;
        Session["contact_list_maillist"] = null;
        Session["contact_list_contact_loc"] = null;
        Session["contact_list_addr1"] = null;
        Session["contact_list_addr2"] = null;
        Session["contact_list_city"] = null;
        Session["contact_list_state"] = null;
        Session["contact_list_zip"] = null;
        Session["contact_list_country"] = null;
        Session["contact_list_county"] = null;
        Session["contact_list_territory"] = null;
        Session["contact_list_access_code"] = null;
        Session["contact_list_cell_phone"] = null;
        Session["contact_list_fax"] = null;
        Session["contact_list_email"] = null;
        Session["contact_list_website"] = null;
        Session["contact_list_comp_code"] = null;
        Session["contact_list_status_code"] = null;
        Session["contact_list_sic_code"] = null;
        Session["contact_comp_des"] = null;
        Session["contact_status_des"] = null;
        Session["contact_sic_des"] = null;

       
    }

    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
       
        Session["dbGrid_contact_CurrentPageIndex"] = 0;

        Session["contact_list_index"] = null;
        Session["contact_list_cust_no"] = null;
        Session["contact_list_last_name"] = null;
        Session["contact_list_first_name"] = null;
        Session["contact_list_contact_title"] = null;
        Session["contact_list_ship_id"] = null;
        Session["contact_list_type"] = null;
        Session["contact_list_phone"] = null;
        Session["contact_list_extension"] = null;
        Session["contact_list_cust_name"] = null;
        Session["contact_list_sman"] = null;
        Session["contact_list_middle_initial"] = null;
        Session["contact_list_sirname"] = null;
        Session["contact_list_maillist"] = null;
        Session["contact_list_contact_loc"] = null;
        Session["contact_list_addr1"] = null;
        Session["contact_list_addr2"] = null;
        Session["contact_list_city"] = null;
        Session["contact_list_state"] = null;
        Session["contact_list_zip"] = null;
        Session["contact_list_country"] = null;
        Session["contact_list_county"] = null;
        Session["contact_list_territory"] = null;
        Session["contact_list_access_code"] = null;
        Session["contact_list_cell_phone"] = null;
        Session["contact_list_fax"] = null;
        Session["contact_list_email"] = null;
        Session["contact_list_website"] = null;
        Session["contact_list_comp_code"] = null;
        Session["contact_list_status_code"] = null;
        Session["contact_list_sic_code"] = null;
        Session["contact_comp_des"] = null;
        Session["contact_status_des"] = null;
        Session["contact_sic_des"] = null;
        

    }

    private void ClearSession()
    {
        Session["dbGrid_contact_Sort"] = null;

        Session["dbGrid_contact_SearchSQL"] = null;
        Session["dbGrid_contact_SearchParams"] = null;

        Session["htPeramcontact"] = null;
        Session["dbGrid_contact_CurrentPageIndex"] = null;
        Session["dbGrid_contact_CurrentPageCount"] = null;

        Session["dbGrid_contact_SortExpression"] = null;
        Session["dbGrid_contact_SortDirection"] = null;
    }

    protected void ShowWait()
    {
        Response.Write("<div id='mydiv' align=center>&nbsp;</div>");
        Response.Write("<script>mydiv.innerText = '';</script>");
        Response.Write("<script language=javascript>;");
        Response.Write("var dots = 0;var dotmax = 10;function ShowWait()");
        Response.Write("{var output; output = '" + "Please wait" + "';dots++;if(dots>=dotmax)dots=1;");
        Response.Write("for(var x = 0;x < dots;x++){output += '.';}mydiv.innerText =  output;}");
        Response.Write("function StartShowWait(){mydiv.style.visibility = 'visible'; window.setInterval('ShowWait()',500);}");
        Response.Write("function HideWait(){mydiv.style.visibility = 'hidden';window.clearInterval();}");
        Response.Write("StartShowWait();</script>");
        Response.Flush();
    }


    protected void dbGrid_contact_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["contact_list_index"] = dbGrid_contact.SelectedIndex;
        try
        {
            Session["cust_type_comp"] = Session["cust_type_comp"];
            Session["contact_list_cust_no"] = dbGrid_contact.SelectedRow.Cells[29].Text;
            Session["contact_list_last_name"] = dbGrid_contact.SelectedRow.Cells[26].Text;
            Session["contact_list_first_name"] = dbGrid_contact.SelectedRow.Cells[27].Text;
            Session["contact_list_contact_title"] = dbGrid_contact.SelectedRow.Cells[28].Text;
            Session["contact_list_ship_id"] = dbGrid_contact.SelectedRow.Cells[30].Text;
            Session["contact_list_type"] = dbGrid_contact.SelectedRow.Cells[31].Text;
            Session["contact_list_phone"] = dbGrid_contact.SelectedRow.Cells[32].Text;
            Session["contact_list_extension"] = dbGrid_contact.SelectedRow.Cells[33].Text;
            Session["contact_list_cust_name"] = dbGrid_contact.SelectedRow.Cells[34].Text;


            foreach (GridViewRow gv in dbGrid_contact.Rows)
            {
                Session["contact_list_sman"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label1")).Text;
                Session["contact_list_middle_initial"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label2")).Text;
                Session["contact_list_sirname"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label3")).Text;
                Session["contact_list_maillist"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label4")).Text;
                Session["contact_list_contact_loc"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label5")).Text;
                Session["contact_list_addr1"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label6")).Text;
                Session["contact_list_addr2"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label7")).Text;
                Session["contact_list_city"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label8")).Text;
                Session["contact_list_state"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label9")).Text;

                Session["contact_list_zip"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label10")).Text;
                Session["contact_list_country"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label11")).Text;
                Session["contact_list_county"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label12")).Text;
                Session["contact_list_territory"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label13")).Text;
                Session["contact_list_access_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label14")).Text;
                Session["contact_list_cell_phone"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label15")).Text;

                Session["contact_list_fax"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label16")).Text;
                Session["contact_list_email"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label17")).Text;
                Session["contact_list_website"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label18")).Text;
                //Response.Write(Session["contact_list_cust_no"]);
                Session["contact_list_comp_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label19")).Text;
                Session["contact_list_status_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label20")).Text;
                Session["contact_list_sic_code"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label21")).Text;
                Session["contact_rec_key"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label22")).Text;

                Session["contact_comp_des"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label23")).Text;
                Session["contact_status_des"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label24")).Text;
                Session["contact_sic_des"] = ((Label)dbGrid_contact.SelectedRow.FindControl("Label25")).Text;

            }


        }
        catch {  }
    }


    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView3.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
        
    }

    public SortDirection GridViewSortDirection
    {

        get
        {

            if (ViewState["sortDirection"] == null)

                ViewState["sortDirection"] = SortDirection.Ascending;

            return (SortDirection)ViewState["sortDirection"];

        }

        set { ViewState["sortDirection"] = value; }

    }
    protected void GridView1_Sorting(object sender, GridViewSortEventArgs e)
    {

        string sortExpression = e.SortExpression;

        if (GridViewSortDirection == SortDirection.Ascending)
        {

            GridViewSortDirection = SortDirection.Descending;

            SortGridView(sortExpression, " DESC");

        }

        else
        {

            GridViewSortDirection = SortDirection.Ascending;

            SortGridView(sortExpression, " ASC");

        }

    }

    private void SortGridView(string sortExpression, string direction)
    {
        Session["contact_list_grid_sort_ex"] = sortExpression + direction;
        
    }




    protected void lnk_viewcontacts_click(object sender, EventArgs e)
    {
        Response.Redirect("view_contacts.aspx");
    }
    protected void lnk_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("list_notes.aspx");
    }
    protected void lnk_MailList_click(object sender, EventArgs e)
    {
        Response.Redirect("contact_maillist.aspx");
    }
    protected void lnk_calendar_click(object sender, EventArgs e)
    {
        Response.Redirect("appointment.aspx");
    }
    protected void DeleteAll_click(object sender, EventArgs e)
    {
        //SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        //try
        //{
        //    conn.Open();
        //    string cmd = "select * from contact";
        //    SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
        //    DataSet ds = new DataSet();
        //    da.Fill(ds);
        //    DataTable tab = ds.Tables[0];
        //    DataRow dr;
        //    dr = tab.NewRow();
        //    dr["company"] = "001";
        //    dr["rec_key"] = "4";
        //    tab.Rows.Add(dr);
        //    SqlCommandBuilder bld = new SqlCommandBuilder(da);
        //    da.Update(ds);
        //    //foreach (DataRow objRow in tab)
        //    //{

        //    //    DataRow row;
        //    //    row = dtNew.NewRow();
        //    //    row["company"] = objRow["company"];
        //    //    row["cust_no"] = objRow["cust-no"];
        //    //    row["sman"] = objRow["sman"];
        //    //    row["first_name"] = objRow["first-name"];
        //    //    row["contact_title"] = objRow["contact-title"];
        //    //    row["type"] = objRow["type"];
        //    //    row["contact_loc"] = objRow["contact-loc"];
        //    //    row["cust_name"] = objRow["cust-name"];
        //    //    row["addr1"] = objRow["addr1"];
        //    //    row["addr2"] = objRow["addr2"];
        //    //    row["city"] = objRow["city"];
        //    //    row["state"] = objRow["state"];
        //    //    row["zip"] = objRow["zip"];
        //    //    row["territory"] = objRow["territory"];
        //    //    row["phone"] = objRow["phone"];
        //    //    row["fax"] = objRow["fax"];
        //    //    row["extension"] = objRow["extension"];
        //    //    row["email"] = objRow["email"];
        //    //    row["rec_key"] = objRow["rec_key"];

        //    //    Response.Write(row["company"]);
        //    //    dtNew.Rows.Add(row);

        //    //}
        //    conn.Close();


        //    dbGrid_contact.DataSource = ds;
        //    dbGrid_contact.DataBind();
        //}
        //catch
        //{ return; }
        //finally
        //{
        //    conn.Close();
        //}
    }
}
